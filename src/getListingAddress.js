if (!process.env.GEOCODER_API_KEY) throw new Error('Missing GEOCODER_API_KEY')

const Geocoder = require('node-geocoder')

const geocoder = Geocoder({
	provider: 'google',
	httpAdapter: 'https',
	apiKey: process.env.GEOCODER_API_KEY,
	formatter: 'json'
})

/** @type {(listing: Record<string, string>) => Promise<Record<string, string>>} */
const getListingAddress = async listing => {
	const results = await geocoder.reverse({
		lat: Number.parseFloat(listing.latitude),
		lon: Number.parseFloat(listing.longitude)
	})

	const result = results[0]
	if (!result) return null

	return {
		address: result.formattedAddress ?? '',
		street_number: result.streetNumber ?? '',
		street_name: result.streetName ?? '',
		country: result.country ?? '',
		zip_code: result.zipcode ?? '',
		neighborhood: result.extra?.neighborhood ?? '',
		county: result.administrativeLevels?.level2long ?? '',
		state: result.administrativeLevels?.level1long ?? ''
	}
}

module.exports = getListingAddress
