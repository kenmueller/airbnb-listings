if (!process.env.USER_AGENT) throw new Error('Missing USER_AGENT')

const cheerio = require('cheerio')

/** @type {(listingId: string, browser: import('puppeteer').Browser) => Promise<Record<string, string> | null>} */
const getListingInfo = async (listingId, browser) => {
	const page = await browser.newPage()

	try {
		await page.setUserAgent(process.env.USER_AGENT)

		await page.goto(`https://www.airbnb.com/rooms/${listingId}`, {
			waitUntil: 'networkidle0'
		})

		if (!new URL(page.url()).searchParams.has('source_impression_id')) {
			// Listing does not exist, no source_impression_id
			return null
		}

		const $ = cheerio.load(await page.content())

		const data = $('.lgx66tx')
			.first()
			.text()
			.split('·')
			.map(s => s.trim())
			.filter(Boolean)
			.reduce((data, line) => {
				const [value, key] = line.split(' ')
				data[key] = value // Keep as string
				return data
			}, {})

		return {
			guests: data.guests ?? data.guest ?? '',
			beds: data.beds ?? data.bed ?? '',
			baths: data.baths ?? data.bath ?? ''
		}
	} finally {
		await page.close()
	}
}

getListingInfo.empty = {
	guests: '',
	beds: '',
	baths: ''
}

module.exports = getListingInfo
