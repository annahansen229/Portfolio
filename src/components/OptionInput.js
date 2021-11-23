import React from "react"

export default function OptionInput(props) {
    const { options, handleChange } = props
    
    // let selectElements = options.map(([attribute, choices], index) => {
    let selectElements = Object.keys(options).map((attribute, index) => {
        // generate the option elements to go inside the select element
        // let optionElement = choices.map((choice) => {
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
        return (
            <div className='search-param' key={index}>
                <label htmlFor={attribute}>{attribute}</label>
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
