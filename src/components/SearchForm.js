import React from "react"

import OptionInput from "./OptionInput"

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
            species: new Set(),
            house: new Set(),
            ancestry: new Set(),
            book: new Set(),
        }

        // fill out the character-based option sets
        for (const thisCharacter of characters) {
            // console.log(thisCharacter)
            myOptions.species.add(thisCharacter.species)
            myOptions.house.add(thisCharacter.house)
            myOptions.ancestry.add(thisCharacter.ancestry)
        }
        
        // fill out the book-based option set
        for (const thisBook of books) {
            myOptions.book.add(thisBook.title)
        }

        return myOptions
    }

    CreateOptions(options, attribute) {
        // return the option elements for the selected attribute
        
        let element = options[{attribute}].map((value) => {
            return (
                <option value={value}>{value}</option>
            )
        })

        return (
            <div>
                <label htmlFor={attribute}>{attribute}:</label>
                <select 
                    name={attribute}
                    id={attribute}
                >
                    <option value='placeholder'>Placeholder</option>
                </select>
            </div>
        )
    }

    async componentDidMount(){
        const myOptions = await this.setOptions()
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
                        <OptionInput 
                            options={options}
                            attribute='species'
                        />

                        {/* <this.CreateOptions 
                            options={options}
                            attribute='species'
                        /> */}

                        {/* <this.CreateOptions attribute='ancestry' /> */}
                        
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

                        {/* <this.CreateOptions attribute='house' /> */}

                        <input
                            type='checkbox'
                            id='alive'
                        />
                        <label htmlFor='living'>Living</label>

                        {/* <this.CreateOptions attribute='book' /> */}

                    </form>
                }
            </React.Fragment>
        )
    }
}