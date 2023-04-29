if (!process.env.USER_AGENT) throw new Error('Missing USER_AGENT')

const getListingUrl = require('./getListingUrl')

/** @type {(listingId: string, browser: import('puppeteer').Browser) => Promise<Record<string, string> | null>} */
const getListingInfo = async (listingId, browser) => {
	const page = await browser.newPage()

	try {
		await page.setUserAgent(process.env.USER_AGENT)

		await page.goto(getListingUrl(listingId), {
			waitUntil: 'networkidle0'
		})

		const errorMessage = await page.evaluate(
			() =>
				document.querySelector(
					'.row.space-top-8.space-8.row-table > .col-5.col-middle > h2'
				)?.textContent
		)

		if (
			errorMessage ===
			"We can't seem to find the page you're looking for."
		) {
			// Listing does not exist
			return null
		}

		const text = await page.evaluate(
			() => document.querySelector('.lgx66tx')?.textContent
		)

		const data = text
			?.split('·')
			.map(s => s.trim())
			.filter(Boolean)
			.reduce((data, line) => {
				const [value, key] = line.split(' ')
				data[key] = value // Keep as string
				return data
			}, {})

		const result = {
			guests: data?.guests ?? data?.guest ?? '',
			beds: data?.beds ?? data?.bed ?? '',
			bedrooms: data?.bedrooms ?? data?.bedroom ?? '',
			baths: data?.baths ?? data?.bath ?? ''
		}

		const validCount = Object.values(result).filter(
			result => result && !Number.isNaN(Number.parseInt(result))
		).length

		if (validCount < 2)
			throw new Error('Less than 2 valid listing info fields')

		return result
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
