const getListings = require('./getListings')
const log = require('./log')

/** @type {() => Promise<void>} */
const main = async () => {
	const listingsOut = await getListings('listings-out.csv')

	const listingsOutWithInfo = listingsOut.filter(listing => {
		const info = {
			guests: listing.guests,
			bed: listing.beds,
			bedrooms: listing.bedrooms,
			baths: listing.baths
		}

		const validCount = Object.values(info).filter(
			result => result && !Number.isNaN(Number.parseInt(result))
		).length

		return validCount >= 2
	})

	log('Listings with info: ', listingsOutWithInfo.length, 'success')
}

main()
