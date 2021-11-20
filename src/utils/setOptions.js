import { useContext } from "react"
import { UserContext } from "./UserContext"

export default function setOptions() {
    // get context data
    const [books, setBooks] = useContext(UserContext)
    const [characters, setCharacters] = useContext(UserContext)
        
    // create an object to contain the options for the search parameters in the form of {attribute: [option, option, option]}
    // the house entries in the character data have other entries that are not related to Hogwarts like "House Black." These entries are ignored for our purposes and the list is manually set to only include Hogwarts houses.
    // the species and ancestry entries in the data contain multiples so these are instantiated as sets to avoid duplicate entries in the results. 
    let myOptions = {
        house: ['', 'Gryffindor', 'Hufflepuff', 'Ravenclaw', 'Slytherin'],
        species: new Set(),
        ancestry: new Set(),
        book: [],
    }

    // fill out the character-based option sets
    for (const thisCharacter of characters) {
        
        // species option
        // data clean-up
        // a bunch of entries in the data have multiple species and they are listed in different ways, like "Half-Giant/Half-Human" and then "Half-Human/Half-Giant". This method will identify the species keywords in the entry and push those key words instead of pushing the multiple variations. This will result in a smaller and more concise set of options. An entry with multiple species keywords will result in a push for each species key word. The above examples will result in both "Human" and "Giant" being pushed. The later filter operation to return matching entries will utilize partial matches. A search for "Human"  will return any entry with the word 'human' in it, inlcuding "Human," "Half-Human/Half-Giant," "Half-Giant/Half-Human," etc.

        let thisSpecies = thisCharacter.species
        let addedSpecies = false

        const regexHuman = new RegExp('human', 'gi')
        const regexKneazle = new RegExp('kneazle', 'gi')
        const regexGoblin = new RegExp('goblin', 'gi')
        const regexTroll = new RegExp('troll', 'gi')
        const regexGiant = new RegExp('giant', 'gi')
        const regexCat = new RegExp('cat', 'gi')
        const regexOwl = new RegExp('owl', 'gi')
        const regexDog = new RegExp('dog', 'gi')
        const regexWerewolf = new RegExp('werewolf', 'gi')
        const regexVeela = new RegExp('veela', 'gi')
        const regexPortrait = new RegExp('portrait', 'gi')
        const regexSelkie = new RegExp('selkie', 'gi')
        const regexHouseElves = new RegExp('house-elves', 'gi')
        
        // if thisSpecies is not blank
        if (thisSpecies) {
            // test if the species needs to be replaced. If it does, add the replacement string to the set and set the addedSpecies boolean to true
            if (thisSpecies.match(regexHuman)) {
                myOptions.species.add('Human')
                addedSpecies = true
            }

            if (thisSpecies.match(regexKneazle)) {
                myOptions.species.add('Kneazle')
                addedSpecies = true
            }

            if (thisSpecies.match(regexGoblin)) {
                myOptions.species.add('Goblin')
                addedSpecies = true
            }
                
            if (thisSpecies.match(regexTroll)) {
                myOptions.species.add('Troll')
                addedSpecies = true
            }
            
            if (thisSpecies.match(regexGiant)) {
                myOptions.species.add('Giant')
                addedSpecies = true
            }
                
            if (thisSpecies.match(regexCat)) {
                thisSpecies.match(regexCat)
                addedSpecies = true
            }
            
            if (thisSpecies.match(regexOwl)) {
                myOptions.species.add('Owl')
                addedSpecies = true
            }
            
            if (thisSpecies.match(regexDog)) {
                myOptions.species.add('Dog')
                addedSpecies = true
            }
            
            if (thisSpecies.match(regexWerewolf)) {
                myOptions.species.add('Werewolf')
                addedSpecies = true
            }
            
            if (thisSpecies.match(regexVeela)) {
                myOptions.species.add('Veela')
                addedSpecies = true
            }
                
            if (thisSpecies.match(regexPortrait)) {
                myOptions.species.add('Portrait')
                addedSpecies = true
            }
                
            if (thisSpecies.match(regexSelkie)) {
                myOptions.species.add('Selkie')
                addedSpecies = true
            }

            if (thisSpecies.match(regexHouseElves)) {
                myOptions.species.add('House-elf')
                addedSpecies = true
            }
                
            // if the addedSpecies boolean is still false then add the original string
            (!addedSpecies) && myOptions.species.add(thisSpecies)
        }

        // ancestry options
        // data clean-up
        // the ancestry entries in the data have similar issues as described above. The same method is implemented to create a smaller and more consise set of options.
        let thisAncestry = thisCharacter.ancestry
        let addedAncestry = false
        
        const regexMuggle = new RegExp('muggle', 'gi')
        const regexPureBlood = new RegExp('pure-blood', 'gi')
        const regexHalfBlood = new RegExp('half-blood', 'gi')
        const regexMagical = new RegExp('magical', 'gi')
        const regexSquib = new RegExp('squib', 'gi')

        // test if the ancestry needs to be replaced. If it does, add the replacement string and set the addedAncestry boolean to true.
        if (thisAncestry) {
            if (thisAncestry.match(regexMuggle)) {
                myOptions.ancestry.add('Muggle')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexPureBlood)) {
                myOptions.ancestry.add('Pure-blood')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexHalfBlood)) {
                myOptions.ancestry.add('Half-Blood')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexHuman)) {
                myOptions.ancestry.add('Human')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexMagical)) {
                myOptions.ancestry.add('Magical')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexSquib)) {
                myOptions.ancestry.add('Squib')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexGoblin)) {
                myOptions.ancestry.add('Goblin')
                addedAncestry = true
            }
            
            if (thisAncestry.match(regexVeela)) {
                myOptions.ancestry.add('Veela')
                addedAncestry = true
            }

            // if the addedSpecies boolean is still false then add the original string
            (!addedAncestry) && myOptions.ancestry.add(thisAncestry)
        }
    }
    
    // convert the species and ancestry sets to arrays so that array methods can be used on them later, sort them alphabetically for better user experience with the form, and add a blank entry to the front
    myOptions.species = Array.from(myOptions.species)
    myOptions.species = myOptions.species.sort()
    myOptions.species.unshift('')

    myOptions.ancestry = Array.from(myOptions.ancestry)
    myOptions.ancestry = myOptions.ancestry.sort()
    myOptions.ancestry.unshift('')

    // fill out the book-based option set
    // it will be in the order the books were released which is the desired order. There is no need to sort it. 
    for (const thisBook of books) {
        myOptions.book.push(thisBook.title)
    }

    // add a blank entry to the front
    myOptions.book.unshift('')

    return myOptions
    
}