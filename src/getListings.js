const { readFile } = require('fs/promises')
const { parse } = require('csv-parse/sync')
const { join } = require('path')

/** @type {(name: string, max?: number, offset?: number) => Promise<Record<string, string>[]>} */
const getListings = async (name, max, offset) => {
	try {
		const data = await readFile(join(__dirname, '..', 'data', name), {
			encoding: 'utf8'
		})

		/** @type {Record<string, string>[]} */
		const listings = parse(data, { columns: true, skip_empty_lines: true })

		const listingsOffset =
			offset === undefined ? listings : listings.slice(offset)

		const listingsBounded =
			max === undefined ? listingsOffset : listingsOffset.slice(0, max)

		return listingsBounded
	} catch (error) {
		if (error.code === 'ENOENT') return []
		throw error
	}
}

module.exports = getListings
