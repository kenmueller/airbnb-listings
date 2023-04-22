const { readFile } = require('fs/promises')
const { parse } = require('csv-parse/sync')
const { join } = require('path')

const PATH = join(__dirname, '..', 'data', 'listings-in.csv')

/** @type {(max?: number, offset?: number) => Promise<Record<string, string>[]>} */
const getListingsIn = async (max, offset) => {
	const data = await readFile(PATH, { encoding: 'utf8' })

	/** @type {Record<string, string>[]} */
	const listings = parse(data, { columns: true, skip_empty_lines: true })

	const listingsOffset = offset ? listings.slice(offset) : listings
	const listingsBounded = max ? listingsOffset.slice(0, max) : listingsOffset

	return listingsBounded
}

module.exports = getListingsIn
