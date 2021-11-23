export default function filterCharacters(characters, books, form) {
    // takes the API data and filters it according to the user's selection on the form

    console.log(form)

    // start the filtered set as an exact copy of the original
    let filteredCharacters = characters
    
    // the attributes with string values can all use the same filter method
    let stringAttributes = ['name', 'species', 'ancestry', 'house']
    for (const attribute of Object.keys(form)) {
        if (stringAttributes.includes(attribute)) {
            // only apply filters for attributes with a value entered
            if (form[attribute].length > 0) {
                // global case insensitive search allows partial matches such as "weasley" to return the whole Weasley family or "giant" to return any ancestry with giant in it, like half-giant
                let regex = new RegExp(form[attribute], 'gi')
                
                filteredCharacters = filteredCharacters.filter(character => {
                    // this method tests if the character attribute has data before trying to apply the filter to it
                    return character[attribute] ? character[attribute].match(regex) : null
                })
            }
        }
    }
    
    // gender needs a different search method since "female" would be returned when filtering for "male" using .match()
    if (form.gender.length > 0) {
        filteredCharacters = filteredCharacters.filter(character => character.gender.toLowerCase() === form.gender.toLowerCase())
    }

    // A character is introduced in the first entry in the book attribute
    // the book index is one less than the number of the book
    if (form.book.length > 0) {
        filteredCharacters = filteredCharacters.filter(character => {
            let bookIndex = character.books_featured_in[0] - 1
            return books[bookIndex].title === form.book
        })
    }
        
    return filteredCharacters

}