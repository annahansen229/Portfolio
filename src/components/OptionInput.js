export default function OptionInput(props) {
    const { options } = props
    console.log(typeof(options))
    
    // let selectElements = options.map(([attribute, choices], index) => {
    let selectElements = Object.keys(options).map((attribute, index) => {
        // generate the option elements to go inside the select element
        // let optionElement = choices.map((choice) => {
        let optionElement = options[attribute].map((choice) => {
            return (
                <option value={choice}>{choice}</option>
            )
        })

        // generate the select element
        return (
            <div className='select-option' key={index}>
                <label htmlFor={attribute}>{attribute}</label>
                <select 
                    name={attribute}
                    id={attribute}
                >
                    {optionElement}
                </select>
            </div>
        )
    })
    
    return <fieldset>{selectElements}</fieldset>
}
    
    // return (
    //     <div>
    //         <label htmlFor='species'>Species</label>
    //         <select 
    //             name='species'
    //             id='species'
    //         >
    //             <option value='Human'>Human</option>
    //             <option value='Non-Human'>Non-Human</option>
    //         </select>
    //     </div>
    // )
