import React, { useState, useEffect } from "react";
import Navbar from "../components/navbar";
import SpellSearch from "../components/SpellSearch";

const spellsimg = `https://static.wikia.nocookie.net/harrypotter/images/0/0e/Alohomora.gif/revision/latest/scale-to-width-down/490?cb=20140811055322`;

function Spells(props) {
  const spellData = SpellSearch();
  //console.log(spellData[1]); //woohoo!!! now


  const [query, setQuery] = useState("");
  const [spell, setSpell] = useState(null); //get spell
  const [use, setUse] = useState(null); //get spell use

  async function getSpell(props) {
    console.log(props);
    let url = `https://fedeperin-harry-potter-api-en.herokuapp.com/spells/${query}`;
    console.log(url);

    console.log("query", query); // this query will be attached to url
    try {
      const response = await fetch(url);
      const data = await response.json();
      console.log(data);
      console.log(data.spell, data.use); //get the spell name and the use of the spell
      setSpell(data.spell);
      setUse(data.use);
    } catch (error) {
      console.error(error);
    }
  }

  useEffect(() => {
    getSpell();
  }, [query]);

  function handleChange(event) {
    event.preventDefault();
    for (let i = 0; i < 72; i++) {
      if (event.target.value === spellData[i].spell) {
        console.log(spellData[i].id);
        setQuery(spellData[i].id);
      }
    }
  }


  return (
    <div>
      <Navbar />
      <h1 className="spellsheader">Spells</h1>
      <div className="container">
        <form method="get">
          <label className="searchintro" htmlFor="userInput">
            Search for a spell!
          </label>
          <br />
          <input type="text" id="userInput" onChange={handleChange} />
          <input type="reset" value="Reset" />
        </form>
        <div className="searchspell searchresults text-center">{spell}</div>
        <div className="searchresults text-center">{use}</div>

        <div className="listheader text-center">List of Spells</div>
        <ul className="list text-center">
          {" "}
          {spellData.map((item) => {
            return <li>{item.spell}</li>;
          })}
        </ul>
      </div>
    </div>
  );
}

export default Spells;
