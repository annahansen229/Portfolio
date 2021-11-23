import React, { useState, useEffect } from "react";
import Navbar from "../components/navbar";
import PotionSearch from "../components/PotionSearch";


function Potions(props) {
  const potionData = PotionSearch();
  console.log(potionData[1]);
  const [query, setQuery] = useState("");
  const [potion, setPotion] = useState(null); //get potion
  const [description, setDescription] = useState(null); //get potion description

  async function getPotion(props) {
    console.log(props);
    let url = `https://the-harry-potter-database.herokuapp.com/api/1/potions/${query}`;
    console.log(url);

    console.log("query", query); // this query will be attached to url
    try {
      const response = await fetch(url);
      const data = await response.json();
      console.log(data);
      console.log(data[0].id); //get the potion name and the description of the potion
       console.log(data[0].description);
      setPotion(data[0].name);
      setDescription(data[0].description);
    } catch (error) {
      console.error(error);
    }
  }

  useEffect(() => {
    getPotion();
  }, [query]);

  function handleChange(event) {
    event.preventDefault();
    for (let i = 0; i < 150; i++) {
      if (event.target.value === potionData[i].name) {
        console.log(potionData[i].id);
        setQuery(potionData[i].id);
      }
    }
  }

  return (
    <div>
      <Navbar />
      <h1 className="potionsheader">Potions</h1>
      <div className="container">
        <form method="get">
          <label className="potionsearchintro" htmlFor="userInput">
            Search for a potion!
          </label>
          <br />
          <input type="text" id="userInput" onChange={handleChange} />
          <input type="reset" value="Reset" />
        </form>
        <div className="searchpotion searchresults text-center">{potion}</div>
        <div className="searchpotion searchresults text-center">{description}</div>

        <div className="potionlistheader text-center">List of Potions</div>
        <ul className="potionlist text-center">
          {" "}
          {potionData.map((item) => {
            return <li>{item.name}</li>;
          })}
        </ul>
      </div>
    </div>
  );
}

export default Potions;
