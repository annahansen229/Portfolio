import React from "react"
import Character from "../components/Character"
import Navbar from "../components/navbar"

class Search extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            characters: [],
            filteredCharacters: [],
            isLoaded: false,
            hasSearched: false,
        }
    }    

    componentDidMount() {
        
    }

    searchCharacters() {
        
    }

    render() {
        const { filteredCharacters, isLoaded, hasSearched } = this.state
        return ( 
            <React.Fragment>
                <div className='Search'>
                    <h1>Harry Potter App</h1>
                    <Navbar/>
                    <h2>Character Search Page</h2>
                    
                    {!isLoaded && <p>Data Loading...</p> }

                    {isLoaded && 
                        <p>Search Form</p>
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