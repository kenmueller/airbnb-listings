if (!process.env.CHUNK_SIZE) throw new Error('Missing CHUNK_SIZE')
if (!process.env.CHUNK_DELAY) throw new Error('Missing CHUNK_DELAY')

const puppeteer = require('puppeteer')
const chunk = require('lodash/chunk')

const getListingsIn = require('./getListingsIn')
const setListingsOut = require('./setListingsOut')
const getListingInfo = require('./getListingInfo')
const getListingAddress = require('./getListingAddress')
const sleep = require('./sleep')

const MAX_LISTINGS = process.env.MAX_LISTINGS
	? Number.parseInt(process.env.MAX_LISTINGS)
	: undefined

const CHUNK_SIZE = Number.parseInt(process.env.CHUNK_SIZE)
const CHUNK_DELAY = Number.parseFloat(process.env.CHUNK_DELAY) * 1000

/** @type {() => Promise<void>} */
const main = async () => {
	const browser = await puppeteer.launch()

	const listingsIn = await getListingsIn(MAX_LISTINGS)
	const listingsInChunked = chunk(listingsIn, CHUNK_SIZE)

	/** @type {Record<string, string>[]} */
	const listingsOut = []

	let chunksCompleted = 0
	let listingsCompleted = 0

	for (const chunk of listingsInChunked) {
		const results = await Promise.all(
			chunk.map(async listing => {
				const info = await getListingInfo(listing.id, browser)

				if (!info) {
					console.log(
						`Listing #${++listingsCompleted} LISTING NOT FOUND`
					)
					return null
				}

				const address = await getListingAddress(listing)

				if (!address) {
					console.log(
						`Listing #${++listingsCompleted} ADDRESS NOT FOUND`
					)
					return null
				}

				console.log(`Listing #${++listingsCompleted} DONE`)

				return { ...listing, ...info, ...address }
			})
		)

		/** @type {Record<string, string>[]} */
		const resultsPruned = results.filter(Boolean)

		listingsOut.push(...resultsPruned)

		console.log(
			`Chunk #${++chunksCompleted} DONE with ${resultsPruned.length}/${
				results.length
			} listings`
		)

		await sleep(CHUNK_DELAY)
	}

	await setListingsOut(listingsOut)

	await browser.close()
}

main()
