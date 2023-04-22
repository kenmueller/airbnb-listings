/** @type {(ms: number) => Promise<void>} */
const sleep = ms =>
	new Promise(resolve => {
		setTimeout(resolve, ms)
	})

module.exports = sleep
