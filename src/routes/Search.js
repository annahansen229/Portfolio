// import libraries
import React from "react"

// import components
import Navbar from "../components/navbar"
import Character from "../components/Character"
import SearchForm from "../components/SearchForm"

// import utils
import filterCharacters from "../utils/filterCharacters"

// import styles
import "../styles/search.css"

class Search extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            books: [],
            booksLoaded: false,
            characters: [],
            charactersLoaded: false,
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

    formSubmitCallBack = (formData) => {
        // calls the function to filter the character set, sets the state with the filtered set, and sets hasSearched to true so the results can be rendered to the page.
        const {characters, books} = this.state
        this.setState({
            filteredCharacters: filterCharacters(characters, books, formData),
            hasSearched: true
        })
    }

    formResetCallBack = () => {
        // sets hasSearched to false to remove the results from the page
        this.setState({
            hasSearched: false
        })
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
                    <Navbar/>
                    
                    {!charactersLoaded && 
                        <p>Character Data Loading...</p>
                        
                    }

                    {!booksLoaded && 
                        <p>Book Data Loading...</p>
                        
                    }

                    <div className='search-form'>
                        <h3>Search for Harry Potter Characters</h3>
                        {(booksLoaded && charactersLoaded) && 
                            <SearchForm 
                                characters={characters}
                                books={books}
                                submitHandler={this.formSubmitCallBack}
                                resetHandler={this.formResetCallBack}
                            />
                        }
                    </div>
                    
                    <div className='search-results'>
                        <h3>Search Results</h3>
                        {hasSearched && 
                            // search results section isn't rendered until a search has been done
                                <Character 
                                    characterData={filteredCharacters}
                                    books={books} />
                                    
                                }   
                    </div>
                </div>
            </React.Fragment>
        )
    }
}

export default Search