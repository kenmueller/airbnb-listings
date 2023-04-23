const { readFile } = require('fs/promises')
const { parse } = require('csv-parse/sync')
const { join } = require('path')

/** @type {(name: string) => Promise<Record<string, string>[]>} */
const getListings = async name => {
	try {
		const data = await readFile(join(__dirname, '..', 'data', name), {
			encoding: 'utf8'
		})

		return parse(data, { columns: true, skip_empty_lines: true })
	} catch (error) {
		if (error.code === 'ENOENT') return []
		throw error
	}
}

module.exports = getListings
