/** @type {() => Promise<void>} */
const sleepForOneSecond = () =>
	new Promise(resolve => {
		setTimeout(resolve, 1000)
	})

/** @type {(seconds: number) => Promise<void>} */
const sleep = async seconds => {
	for (let elapsed = 0; elapsed < seconds; elapsed++) {
		process.stdout.clearLine()
		process.stdout.cursorTo(0)
		process.stdout.write(`${elapsed}s / ${seconds}s`)

		await sleepForOneSecond()
	}

	process.stdout.clearLine()
	process.stdout.cursorTo(0)
}

module.exports = sleep
