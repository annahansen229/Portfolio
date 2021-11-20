// import libraries
import React from "react"

// import components
import OptionInput from "./OptionInput"

// import utils

export default class SearchForm extends React.Component {
    constructor(props) {
        super(props)
        this.state = {
            isLoaded: false,
            characters: this.props.characters,
            books: this.props.books,
            options: {},
            name: '',
            species: '',
            ancestry: '',
            gender: '',
            house: '',
            living: '',
            book: ''
        }
    }

    handleChange(event) {
        this.setState({
            [event.target.name]: event.target.value
        })
    }

    handleSubmit(event) {
        event.preventDefault()
        console.log(this.state)
    }

    setOptions () {
        const { books, characters } = this.state
        // create an object to contain the options for the search parameters in the form of {attribute: [option, option, option]}

        let myOptions = {
            house: ['Gryffindor', 'Hufflepuff', 'Ravenclaw', 'Slytherin'],
            species: new Set(),
            ancestry: new Set(),
            book: [],
        }

        // fill out the character-based option sets
        for (const thisCharacter of characters) {
            
            // species data clean-up
            let thisSpecies = thisCharacter.species
            let addedSpecies = false

            const regexHuman = new RegExp('human', 'gi')
            const regexKneazle = new RegExp('kneazle', 'gi')
            const regexGoblin = new RegExp('goblin', 'gi')
            const regexTroll = new RegExp('troll', 'gi')
            const regexGiant = new RegExp('giant', 'gi')
            const regexCat = new RegExp('cat', 'gi')
            const regexOwl = new RegExp('owl', 'gi')
            const regexDog = new RegExp('dog', 'gi')
            const regexWerewolf = new RegExp('werewolf', 'gi')
            const regexVeela = new RegExp('veela', 'gi')
            const regexPortrait = new RegExp('portrait', 'gi')
            const regexSelkie = new RegExp('selkie', 'gi')
            const regexHouseElves = new RegExp('house-elves', 'gi')
            
            // if thisSpecies is not blank
            if (thisSpecies) {
                // test if the species needs to be replaced. If it does, add the replacement string to the set and set the addedSpecies boolean to true
                if (thisSpecies.match(regexHuman)) {
                    myOptions.species.add('Human')
                    addedSpecies = true
                }

                if (thisSpecies.match(regexKneazle)) {
                    myOptions.species.add('Kneazle')
                    addedSpecies = true
                }

                if (thisSpecies.match(regexGoblin)) {
                    myOptions.species.add('Goblin')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexTroll)) {
                    myOptions.species.add('Troll')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexGiant)) {
                    myOptions.species.add('Giant')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexCat)) {
                    thisSpecies.match(regexCat)
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexOwl)) {
                    myOptions.species.add('Owl')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexDog)) {
                    myOptions.species.add('Dog')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexWerewolf)) {
                    myOptions.species.add('Werewolf')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexVeela)) {
                    myOptions.species.add('Veela')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexPortrait)) {
                    myOptions.species.add('Portrait')
                    addedSpecies = true
                }
                 
                
                if (thisSpecies.match(regexSelkie)) {
                    myOptions.species.add('Selkie')
                    addedSpecies = true
                }

                if (thisSpecies.match(regexHouseElves)) {
                    myOptions.species.add('House-elf')
                    addedSpecies = true
                }
                 
                // if the addedSpecies boolean is still false add the original string
                (!addedSpecies) && myOptions.species.add(thisSpecies)
            }

            // ancestry data clean-up
            let thisAncestry = thisCharacter.ancestry
            let addedAncestry = false
            
            const regexMuggle = new RegExp('muggle', 'gi')
            const regexPureBlood = new RegExp('pure-blood', 'gi')
            const regexHalfBlood = new RegExp('half-blood', 'gi')
            const regexMagical = new RegExp('magical', 'gi')
            const regexSquib = new RegExp('squib', 'gi')

            console.log(thisAncestry)
            // test if the ancestry needs to be replaced. If it does, add the replacement string and set the addedAncestry boolean to true.
            if (thisAncestry) {
                if (thisAncestry.match(regexMuggle)) {
                    myOptions.ancestry.add('Muggle')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexPureBlood)) {
                    myOptions.ancestry.add('Pure-blood')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexHalfBlood)) {
                    myOptions.ancestry.add('Half-Blood')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexHuman)) {
                    myOptions.ancestry.add('Human')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexMagical)) {
                    myOptions.ancestry.add('Magical')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexSquib)) {
                    myOptions.ancestry.add('Squib')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexGoblin)) {
                    myOptions.ancestry.add('Goblin')
                    addedAncestry = true
                }
                
                if (thisAncestry.match(regexVeela)) {
                    myOptions.ancestry.add('Veela')
                    addedAncestry = true
                }
    
                // if the addedSpecies boolean is still false add the original string
                (!addedAncestry) && myOptions.ancestry.add(thisAncestry)
            }
        }
        
        // fill out the book-based option set
        for (const thisBook of books) {
            myOptions.book.push(thisBook.title)
        }

        // convert the sets to arrays and sort them alphabetically
        myOptions.species = Array.from(myOptions.species)
        myOptions.species = myOptions.species.sort()

        myOptions.ancestry = Array.from(myOptions.ancestry)
        myOptions.ancestry = myOptions.ancestry.sort()

        return myOptions
    }

    async componentDidMount(){
        const { books, characters } = this.state
        
        // const myOptions = await setOptions(books, characters)
        const myOptions = await this.setOptions(books, characters)
        this.setState ({
            options: myOptions,
            isLoaded: true,
        })
    }

    render( ) {
        const { isLoaded, options }=this.state
        
        return(
            <React.Fragment>
                {!isLoaded && <p>Loading Data</p>}

                {isLoaded && 
                    <form>
                        <label htmlFor='name'>Name:</label>
                        <input 
                            type='text'
                            name='name'
                            id='name'
                        />
                                                
                        <fieldset>
                            <legend>Gender</legend>
                            <input
                                type='radio'
                                name='gender'
                                value='male'
                            />
                            <label htmlFor='male'>Male</label>

                            <input
                                type='radio'
                                name='gender'
                                value='femal'
                            />
                            <label htmlFor='female'>Female</label>
                        </fieldset>

                        <input
                            type='checkbox'
                            id='alive'
                        />
                        <label htmlFor='living'>Living</label>

                        <OptionInput options={options} />

                    </form>
                }
            </React.Fragment>
        )
    }
}