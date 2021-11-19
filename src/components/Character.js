
function Character(props) {
    // Function to return characters after search

    // props:
    // characterData: array of characters
    
    const { characterData } = this.props

    let element = characterData.map (
        (character, index) => {
            return (
                <div className='flip-card'>
                    <div className='flip-card-inner'>
                        <div className='flip-card-front'>
                            <h3> {character.name} </h3>
                            <table>
                                <Tablerows 
                                    character={character}
                                    card='front'
                                />
                            </table>    
                        </div>
                        <div className='flip-card-back'>
                            <h3> {character.name} </h3>
                            <table>
                            <Tablerows 
                                    character={character}
                                    card='back'
                                />
                            </table>
                        </div>
                    </div>
                </div>
            )
        }

    )

    return element
    
}

function Tablerows(character, card) {
    // function to return set of table rows of key, value from character

    const keysfront = ['name', 'birth', 'death', 'species', 'ancestry', 'gender', 'hair_color', 'eye_color']

    const keysback = ['wand', 'patronus', 'house', 'associated_groups', 'books_featured_in']
    
    if (card === 'front') {
        for (const [key, value] of Object.entries(character)) {
            if (keysfront.includes(key)) {
                <tr>
                    <td>{key.replace(/_/g, ' ')}:</td>
                    <td>{value}</td>
                </tr>
            }
        }
    } else { // card === 'back'
        for (const [key, value] of Object.entries(character)) {

            if (keysback.includes(key)) {
                <tr>
                    <td>{key.replace(/_/g, ' ')}:</td>
                    <td>{value}</td>
                </tr>
            }
        }
    }
}
export default Character