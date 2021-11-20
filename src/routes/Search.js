// import libraries
import React from "react"

// import components
import Navbar from "../components/navbar"
import Character from "../components/Character"
import SearchForm from "../components/SearchForm"

// import utilities

class Search extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            books: [],
            characters: [],
            filteredCharacters: [],
            charactersLoaded: false,
            booksLoaded: false,
            hasSearched: false,
        }
    }    

    componentDidMount() {
        // const url = 'https://the-harry-potter-database.herokuapp.com/api/1/characters/all'
        const url_characters = 'https://the-harry-potter-database.herokuapp.com/api/1/characters/756' // Albus Dumbledore
        

        const url_books = 'https://the-harry-potter-database.herokuapp.com/api/1/books/all'


        fetch(url_characters)
        .then((response) => response.json())
        .then((data) => {
            this.setState ({
                characters: data,
                charactersLoaded: true
            })
            console.log('got characters in Search', this.state.characters)
        })

        fetch(url_books)
        .then((response) => response.json())
        .then((data) => {
            this.setState ({
                books: data,
                booksLoaded: true
            })
            console.log('got books in Search', this.state.books)
        })


        
        console.log('Search componentDidMount')
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