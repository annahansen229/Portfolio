import React, { useEffect, useState } from "react";
import "bootstrap/dist/css/bootstrap.min.css";
import "../App.css";
import Navbar from "../components/navbar.js";


function Spells() {

  return (
    <div className="Spells">
      <Navbar />
      <body className="spellsbody">
        <h1 className="spellsheader"> Spells</h1>
      </body>
    </div>
  );
}

export default Spells;

