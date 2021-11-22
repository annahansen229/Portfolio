// import libraries
import React from "react"

// import components
import Navbar from "../components/navbar"
import Character from "../components/Character"
import SearchForm from "../components/SearchForm"

// import utilities
import setOptions from "../utils/setOptions"

class Search extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            books: [],
            booksLoaded: false,
            characters: [],
            charactersLoaded: false,
            options: [],
            optionsLoaded: false,
            filteredCharacters: [],
            hasSearched: false,
        }
    }    

    componentDidMount() {
        const url_characters = 'https://the-harry-potter-database.herokuapp.com/api/1/characters/all'
        
        // const url_characters = 'https://the-harry-potter-database.herokuapp.com/api/1/characters/756' // Albus Dumbledore for 

        const url_books = 'https://the-harry-potter-database.herokuapp.com/api/1/books/all'

        fetch(url_characters)
        .then((response) => response.json())
        .then((data) => {
            this.setState ({
                characters: data,
                charactersLoaded: true
            })
        })

        fetch(url_books)
        .then((response) => response.json())
        .then((data) => {
            this.setState ({
                books: data,
                booksLoaded: true
            })
        })
    }

    searchCharacters() {
    }

    render() {
        const { books, 
                characters, 
                filteredCharacters, 
                booksLoaded, 
                charactersLoaded,
                hasSearched 
            } = this.state
        return ( 
            <React.Fragment>
                <div className='Search'>
                    <h1>Harry Potter App</h1>
                    <Navbar/>
                    <h2>Character Search Page</h2>
                    
                    {!charactersLoaded && 
                        <p>Character Data Loading...</p>
                        
                    }

                    {!booksLoaded && 
                        <p>Book Data Loading...</p>
                        
                    }

                    {booksLoaded && charactersLoaded && 
                        <div>
                            <h3>Search for Harry Potter Characters</h3>
                            
                            <SearchForm 
                                characters={characters}
                                books={books}
                            />
                        </div>

                    }
                    
                    {hasSearched && 
                        // search results section isn't rendered until a search has been done
                        <Character characterData={filteredCharacters} />
                    }   
                </div>
            </React.Fragment>
        )
    }
}

export default Search