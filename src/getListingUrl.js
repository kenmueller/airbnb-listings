/** @type {(listingId: string) => string} */
const getListingUrl = listingId => `https://www.airbnb.com/rooms/${listingId}`

module.exports = getListingUrl
