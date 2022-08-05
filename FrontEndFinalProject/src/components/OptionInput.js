import React from "react"

export default function OptionInput(props) {
    // this function returns a <select> parent with it's <option> children from the given options array

    // props
    // options: an object in the form of {attribute: [choice, choice, choice...]} 
    // handleChange: the parent form's function for handling a change to the select element
    const { options, handleChange } = props
    
    let selectElements = Object.keys(options).map((attribute, index) => {
        // generate the option elements to go inside the select element
        let optionElement = options[attribute].map((choice, attrIndex) => {
            let uniqueKey = `${attribute}:${attrIndex}`
            return (
                <option 
                    value={choice}
                    key={uniqueKey}
                >
                    {choice}
                </option>
            )
        })

        // generate the select element
        // the select fields all use their {attribute} for the label text, except book, which uses a different string
        let labelText 
        if (attribute === 'book') {
            labelText = "First appeared in"
        } else {
            labelText = attribute
        }
        
        return (
            // generate the select element with the {optionElement} children
            <div className='search-param' key={index}>
                <label className='search select' htmlFor={attribute}>{labelText}</label>
                <select 
                    name={attribute}
                    id={attribute}
                    onChange={handleChange}
                >
                    {optionElement}
                </select>
            </div>
        )
    })
    
    return <React.Fragment>{selectElements}</React.Fragment>
}
