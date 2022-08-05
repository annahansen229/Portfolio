import { useState, useEffect } from "react";

function PotionSearch() {
  const url = `https://the-harry-potter-database.herokuapp.com/api/1/potions/all/`;
  let [potionnames, setPotionnames] = useState([]);

  useEffect(() => {
    fetch(url)
      .then((res) => res.json())
      .then((data) => {
        console.log(data);
    
        setPotionnames(data);
      })
      .catch((error) => console.log(error));
  }, [url]); //can put url here but it never changes

  const potionData = potionnames;
  return potionData;
}

export default PotionSearch;
