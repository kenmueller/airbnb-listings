const chalk = require('chalk')

/** @type {(prefix: string, message: 'string', status: 'success' | 'error') => void} */
const log = (prefix, message, status) => {
	console[status === 'success' ? 'log' : 'error'](
		chalk[status === 'success' ? 'green' : 'red'](prefix + message)
	)
}

module.exports = log
