export default function OptionElement(props) {
    const { options } = props
    console.log(typeof(options))
    
    let optionElement = options.map((choice) => {
        return (
            <option value={choice}>{choice.toUpper()}</option>
        )
    })

    return {optionElement}
}
    
    
