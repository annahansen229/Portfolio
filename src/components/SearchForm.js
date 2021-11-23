// import libraries
import { useState } from "react"

// import components
import setOptions from "../utils/setOptions"
import OptionInput from "./OptionInput"

export default function SearchForm(props) {
    const {books, characters, submitHandler, resetHandler} = props

    const [form, setForm] = useState({
        // a local form state which mirrors the context form state and form input labels
        name: '',
        species: '',
        ancestry: '',
        gender: '',
        house: '',
        book: '',
    })

    function handleChange(event){
        // get the change event values
        const { name, value} = event.target
        
        //copy the form state and update it with the changes made
        const updatedForm = {
            ...form, 
            [name]: value
        }
        
        // set the state with the values from the updated copy
        setForm(updatedForm)
    }

    function onReset(event) {
        const blankForm = {
            name: '',
            species: '',
            ancestry: '',
            gender: '',
            house: '',
            book: '',
        }
        setForm(blankForm)
        resetHandler(blankForm)
    }

    function onSubmit(event) {
        event.preventDefault()
        submitHandler(form)
    }

    const choices = setOptions(books, characters)

    return (
        <form>
            <div className='search-param'>
                <label htmlFor='name'>Name:</label>
                <input 
                    type='text'
                    name='name'
                    id='name'
                    onChange={handleChange}
                    
                />
            </div>
            <div className='search-param'>               
                <fieldset>
                    <legend>Gender</legend>
                    <input
                        type='radio'
                        name='gender'
                        value='male'
                        onChange={handleChange}
                    />
                    <label htmlFor='male'>Male</label>

                    <input
                        type='radio'
                        name='gender'
                        value='female'
                        onChange={handleChange}
                    />
                    <label htmlFor='female'>Female</label>
                </fieldset>
            </div>

            <OptionInput 
                options={choices}
                handleChange={handleChange} 
            />

            <input
                type='submit'
                onClick={onSubmit}
            />
            
            <input
                type='reset'
                onClick={onReset}
            />
        </form>
    )
}