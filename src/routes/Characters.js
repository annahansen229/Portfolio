// import libraries
import React from "react"

// import components
import Navbar from "../components/navbar"
import Character from "../components/Character"
import SearchForm from "../components/SearchForm"

// import utils
import filterCharacters from "../utils/filterCharacters"

// import styles
import '../styles/characters.css'

class Characters extends React.Component {

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
        
        const url_books = 'https://the-harry-potter-database.herokuapp.com/api/1/books/all'

        // get the data from the API and set the state variables
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
        // calls the function to filter the character set and sets the state variables
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
                    
                    <div className='search-form'>
                        <h3 className='section-title'>Search for Harry Potter Characters</h3>
                        {/* conditional rendering - Loading message(s) displayed until data is ready */}
                        {!charactersLoaded && 
                            <p>Character Data Loading...</p>
                        }

                        {!booksLoaded && 
                            <p>Book Data Loading...</p>
                        }
                        
                        {/* Search form rendered when data is ready */}
                        {(booksLoaded && charactersLoaded) && 
                            <>
                                <p className='search-instructions'>
                                    Use one or more search values below to see characters from the Harry Potter series.
                                </p>
                                
                                <p className='search-instructions'>
                                    To see all characters in the entire series, leave all the search terms blank.
                                </p>
                                <SearchForm 
                                    characters={characters}
                                    books={books}
                                    submitHandler={this.formSubmitCallBack}
                                    resetHandler={this.formResetCallBack}
                                />
                            </>
                        }
                    </div>
                    
                    <div className='search-results'>
                        <h3 className='section-title'>Search Results</h3>
                        {/* conditional rendering - a message is displayed until a search is performed */}
                        {!hasSearched &&
                            <p className='search-instructions'>
                                Click 'Submit' above to see your search results
                            </p>
                        }

                        {/* search results section rendered after a search has been done */}
                        {hasSearched && 
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

export default Characters