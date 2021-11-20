export default function OptionInput(props) {
    const { options, attribute } = props
    
    console.log(typeof(options[attribute]))

    
    return (
        <div>
            <label htmlFor='species'>Species</label>
            <select 
                name='species'
                id='species'
            >
                <option value='Human'>Human</option>
                <option value='Non-Human'>Non-Human</option>
            </select>
        </div>
    )
}