const { writeFile } = require('fs/promises')
const { join } = require('path')

const PATH = join(__dirname, '..', 'data', 'listings-out.csv')

/** @type {(listings: Record<string, string>[]) => Promise<void>} */
const setListingsOut = async listings => {
	const columns = Object.keys(listings[0])
		.sort((a, b) => a.localeCompare(b))
		.join(',')

	const rows = listings.map(listing =>
		Object.entries(listing)
			.sort(([a], [b]) => a.localeCompare(b))
			.map(([, value]) => value && `"${value.replace(/"/g, '""')}"`)
			.join(',')
	)

	const lines = [columns, ...rows]

	await writeFile(PATH, lines.join('\n'), { encoding: 'utf8' })
}

module.exports = setListingsOut
