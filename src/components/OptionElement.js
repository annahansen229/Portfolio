export default function OptionElement(props) {
    const { options } = props
    console.log(typeof(options))
    
    let optionElement = options.map((choice, index) => {
        return (
            <option value={choice} key={index}>{choice.toUpper()}</option>
        )
    })

    return {optionElement}
}
    
    
