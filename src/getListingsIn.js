const { readFile } = require('fs/promises')
const { parse } = require('csv-parse/sync')
const { join } = require('path')

const PATH = join(__dirname, '..', 'data', 'listings-in.csv')

/** @type {(max?: number) => Promise<Record<string, string>[]>} */
const getListingsIn = async max => {
	const data = await readFile(PATH, { encoding: 'utf8' })
	const listings = parse(data, { columns: true, skip_empty_lines: true })

	return max ? listings.slice(0, max) : listings
}

module.exports = getListingsIn
