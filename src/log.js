const chalk = require('chalk')

const log = (prefix, message, status) => {
	console[status === 'success' ? 'log' : 'error'](
		chalk[status === 'success' ? 'green' : 'red'](prefix + message)
	)
}

module.exports = log
