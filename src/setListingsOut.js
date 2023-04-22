const { writeFile } = require('fs/promises')
const { join } = require('path')

const PATH = join(__dirname, '..', 'data', 'listings-out.csv')

/** @type {(listing: Record<string, string>) => string} */
const listingToRow = listing =>
	Object.entries(listing)
		.sort(([a], [b]) => a.localeCompare(b))
		.map(([, value]) => value && `"${value.replace(/"/g, '""')}"`)
		.join(',')

/** @type {(listings: Record<string, string>[], previousListings: Record<string, string>[]) => Promise<void>} */
const setListingsOut = async (listings, previousListings) => {
	const columns = Object.keys(listings[0])
		.sort((a, b) => a.localeCompare(b))
		.join(',')

	const rows = listings.map(listing =>
		previousListings.some(
			previousListing => previousListing.id === listing.id
		)
			? null // Listing already exists
			: listingToRow(listing)
	)

	/** @type {string[]} */
	const rowsPruned = rows.filter(Boolean)

	const lines = [
		columns,
		...previousListings.map(listingToRow),
		...rowsPruned
	]

	await writeFile(PATH, lines.join('\n'), { encoding: 'utf8' })
}

module.exports = setListingsOut
