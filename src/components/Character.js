
function Character(props) {
    // Function to return characters after search

    // props:
    // characterData: array of characters
    
    const { characterData, books } = props
    
    let element = characterData.map (
        (character, index) => {
            return (
                <div className='flip-card'>
                    <div className='flip-card-inner'>
                        <div className='flip-card-front'>
                            <h3> {character.name} </h3>
                            <table>
                                <tbody>
                                    <Tablerows 
                                        character={character}
                                        card='front'
                                        books={books}
                                    />
                                </tbody>
                            </table>    
                        </div>
                        <div className='flip-card-back'>
                            <h3> {character.name} </h3>
                            <table>
                                <tbody>
                                    <Tablerows 
                                            character={character}
                                            card='back'
                                            books={books}
                                        />
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>
            )
        }

    )

    return element
    
}

function Tablerows(props) {
    const {character, card, books} = props
    
    // function to return set of table rows of key, value from character

    const keysfront = ['birth', 'death', 'species', 'ancestry', 'gender', 'hair_color', 'eye_color']

    const keysback = ['wand', 'patronus', 'house', 'associated_groups', 'books_featured_in']
    
    if (card === 'front') {
        let tableRow = Object.keys(character).map((attribute, index) => {
            if (keysfront.includes(attribute)){
                return (
                    <tr>
                        {/* underscores in the character attributes are replaced with spaces */}
                        <td className='category'>
                            {attribute.replace(/_/g, ' ')}:
                        </td>
                        <td>{character[attribute]}</td>
                    </tr>
                )
            }
        })
        return tableRow
        
    } else { // card === 'back'
        let tableRow = Object.keys(character).map((attribute, index) => {
            if (keysback.includes(attribute)){
                switch (attribute) {
                    case 'books_featured_in':
                        // the character data contains an array of books the character was featured in by number. The number corresponds to the id field in the books API data, and the index of the books API data is equal to the id minus 1 (because the index is zero based). This method creates an array of titles from the book data.
                        let booklist = character[attribute].map((bookIndex) => {
                            return books[bookIndex-1].title
                        })

                        // after creating the array of titles it needs to be formatted as a comma delimted string for display which is done with the .join() method.
                        return (
                            <tr>
                                <td className='category'>
                                    {attribute.replace(/_/g, ' ')}:
                                </td>
                                <td>{booklist.join(', ')}</td>
                            </tr>
                        )
                        
                    case 'associated_groups':
                        // the character data contains an array of groups the character was a member of. The array needs to be formatted as a comma delimited string for display which is done with the .join() method.
                        
                        return (
                            <tr>
                                <td className='category'>
                                    {attribute.replace(/_/g, ' ')}:
                                </td>
                                <td>{character[attribute].join(', ')}</td>
                            </tr>
                        )
                    
                    default:
                        // the other attributes for the back of the card are all handled the same way
                        return (
                            <tr>
                                <td className='category'>
                                    {attribute.replace(/_/g, ' ')}:
                                </td>
                                <td>{character[attribute]}</td>
                            </tr>
                        )
                }
            }
        })
        return tableRow
    }
}


export default Character