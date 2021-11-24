// import libraries
import { useState } from "react"

// import components
import setOptions from "../utils/setOptions"
import OptionInput from "./OptionInput"

export default function SearchForm(props) {
    // this function generates the search form which will be rendered on the Characters page
    
    // props
    // books, characters: contain API data
    // submitHandler, resetHandler: parent callbacks for handling form events
    const {books, characters, submitHandler, resetHandler} = props

    const [form, setForm] = useState({
        // a local form state which mirrors the form input labels
        name: '',
        species: '',
        ancestry: '',
        gender: '',
        house: '',
        book: '',
    })

    function handleChange(event){
        // this function copies the user selections into the state variables when the user makes a change on the form

        // get the change event values
        const { name, value} = event.target
        
        //create a copy of the form state and update it with the changes made
        const updatedForm = {
            ...form, 
            [name]: value
        }
        
        // set the state with the values from the updated copy
        setForm(updatedForm)
    }

    function onReset(event) {
        // this function clears the state variables and invokes the reset handler function on the parent when the user clicks Reset on the form
        const blankForm = {
            name: '',
            species: '',
            ancestry: '',
            gender: '',
            house: '',
            book: '',
        }
        
        // clear the state variable
        setForm(blankForm)

        // invoke the parent reset handler 
        resetHandler()
    }

    function onSubmit(event) {
        // this function invokes the parent submit handler with the selections the user has made on the form when the submit button is clicked
        event.preventDefault()
        submitHandler(form)
    }

    // invoke the function to create the option set that will be passed to the function which creates the <select> elements
    const choices = setOptions(books, characters)

    return (
        // generate the search form
        <form className='search-container'>
            <div className='search-param'>
                <label className='search text' htmlFor='name'>Name</label>
                <input 
                    type='text'
                    name='name'
                    id='name'
                    onChange={handleChange}
                    
                />
            </div>
            <div className='search-param'>               
                <fieldset>
                    <legend className='search'>Gender</legend>
                    <div className='radio-option'>
                        <input
                            type='radio'
                            name='gender'
                            value='male'
                            onChange={handleChange}
                        />
                        <label className='search radio' htmlFor='male'>Male</label>
                    </div>

                    <div className='radio-option'>
                        <input
                            type='radio'
                            name='gender'
                            value='female'
                            onChange={handleChange}
                        />
                        <label className='search radio' htmlFor='female'>Female</label>
                    </div>
                </fieldset>
            </div>

            {/* invoke the function which will generate the <select> elements */}
            <OptionInput 
                options={choices}
                handleChange={handleChange} 
            />
            
            <div className='search-param button-container'>
                <input
                    type='submit'
                    onClick={onSubmit}
                />
                
                <input
                    type='reset'
                    onClick={onReset}
                />
            </div>
        </form>
    )
}