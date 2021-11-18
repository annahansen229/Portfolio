import Image from 'react-bootstrap/Image'

export default function Character(props) {
    if (props.characterData.length > 0) {
        // for each character in the list generate a div containing their image and name

        // props:
        // characterData: array of characters
        
        let element = props.characterData.map ((character, index) => {
            return (
                <div className='character' key={index}>
                    <Image src={character.imageUrl} alt={character.image} rounded/>
                    <h3> {character.fullName} </h3>
                </div>
            )
        })
        return <div className='characters'>{element}</div>

    } else {
        // an empty list was passed to the function
        let element = <p>No results. Please try again.</p>
        return <div className='characters'>{element}</div>
    }
    
    

    
}