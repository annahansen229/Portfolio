import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
// import App from './App';

import { BrowserRouter, Routes, Route } from "react-router-dom"
import Home from "./routes/Home.js"
import Search from './routes/Search'
import Spells from "./routes/Spells.js"

const rootElement = document.getElementById('root')

ReactDOM.render(
  <BrowserRouter>
    <Routes>
      <Route path="/" element={<Home />} />
      <Route path="home" element={<Home />} />
      <Route path="search" element={<Search />} />
      <Route path="spells" element={<Spells />} />
    </Routes>
  </BrowserRouter>,
  rootElement
);
