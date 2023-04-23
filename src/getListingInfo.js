if (!process.env.USER_AGENT) throw new Error('Missing USER_AGENT')

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

		const text = await page.evaluate(
			() => document.querySelector('.lgx66tx').textContent
		)

		const data = text
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
			bedrooms: data.bedrooms ?? data.bedroom ?? '',
			baths: data.baths ?? data.bath ?? ''
		}
	} finally {
		await page.close()
	}
}

getListingInfo.empty = {
	guests: '',
	beds: '',
	bedrooms: '',
	baths: ''
}

module.exports = getListingInfo
