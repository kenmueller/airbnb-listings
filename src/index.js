if (!process.env.CHUNK_SIZE) throw new Error('Missing CHUNK_SIZE')
if (!process.env.CHUNK_DELAY) throw new Error('Missing CHUNK_DELAY')
if (!process.env.SECTION_SIZE) throw new Error('Missing SECTION_SIZE')
if (!process.env.SECTION_DELAY) throw new Error('Missing SECTION_DELAY')

const puppeteer = require('puppeteer')
const chunk = require('lodash/chunk')

const getListings = require('./getListings')
const setListingsOut = require('./setListingsOut')
const getListingInfo = require('./getListingInfo')
const getListingAddress = require('./getListingAddress')
const sleep = require('./sleep')
const log = require('./log')

const CHUNK_SIZE = Number.parseInt(process.env.CHUNK_SIZE)
const CHUNK_DELAY = Number.parseInt(process.env.CHUNK_DELAY)
const SECTION_SIZE = Number.parseInt(process.env.SECTION_SIZE)
const SECTION_DELAY = Number.parseInt(process.env.SECTION_DELAY)

/** @type {() => Promise<void>} */
const main = async () => {
	const browser = await puppeteer.launch({
		args: ['--no-sandbox', '--disable-setuid-sandbox'],
		protocolTimeout: 500_000
	})

	try {
		const previousListingsOut = await getListings('listings-out.csv')

		/** @type {[Record<string, string>, number][]} */
		const listingsInAll = (await getListings('listings-in.csv')).map(
			(listing, index) => [listing, index]
		)

		const listingsInCount = listingsInAll.length

		const listingsIn = listingsInAll.filter(
			([listing]) =>
				// Filter the already loaded listings out
				!previousListingsOut.some(
					previousListing => previousListing.id === listing.id
				)
		)

		const listingsInChunked = chunk(listingsIn, CHUNK_SIZE)

		const listingsOut = []

		let chunksCompleted = 0

		for (const chunk of listingsInChunked) {
			let successfulResults = 0

			const results = await Promise.allSettled(
				chunk.map(async ([listing, index]) => {
					const prefix = `Listing ${index + 1}/${listingsInCount} `

					try {
						const [info, address] = await Promise.all([
							getListingInfo(listing.id, browser),
							getListingAddress(listing)
						])

						if (!(info || address))
							log(
								prefix,
								'LISTING NOT FOUND AND ADDRESS NOT FOUND',
								'error'
							)
						else if (!info)
							log(prefix, 'LISTING NOT FOUND', 'error')
						else if (!address)
							log(prefix, 'ADDRESS NOT FOUND', 'error')
						else log(prefix, 'DONE', 'success')

						if (info && address) successfulResults++

						return {
							...listing,
							...(info ?? getListingInfo.empty),
							...(address ?? getListingAddress.empty)
						}
					} catch (error) {
						log(prefix, `ERROR ${error}`, 'error')
						throw error
					}
				})
			)

			/** @type {Record<string, string>[]} */
			const resultsPruned = results
				.map(result =>
					result.status === 'fulfilled' ? result.value : null
				)
				.filter(Boolean)

			if (resultsPruned.length) {
				listingsOut.push(...resultsPruned)
				await setListingsOut(listingsOut, previousListingsOut)
			}

			log(
				`Chunk #${++chunksCompleted} `,
				`DONE with ${successfulResults}/${results.length} successful listings`,
				successfulResults ? 'success' : 'error'
			)

			await sleep(
				chunksCompleted % SECTION_SIZE === 0
					? SECTION_DELAY
					: CHUNK_DELAY
			)
		}
	} finally {
		await browser.close()
	}
}

main()
