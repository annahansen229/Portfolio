import { useState, useEffect, useCallback } from "react";
import Spells from '../routes/Spells.js'

function SpellSearch() {

const url = `https://fedeperin-harry-potter-api-en.herokuapp.com/spells/`;
let [spellnames, setSpellnames] = useState([]);
let [isLoaded, setIsLoaded] = useState(false);

useEffect(() => {
  fetch(url)
      .then((res) => res.json())
      .then((data) => {
        console.log(data);
      
        setSpellnames(data);
        setIsLoaded(true);
      })
      .catch((error) => console.log(error));
  }, [url]); //can put url here but it never changes

  
  const spellData = spellnames;
  return spellData;

}

export default SpellSearch;
