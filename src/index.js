import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import App from './App';

import { BrowserRouter, Routes, Route } from "react-router-dom"
import Home from "./routes/Home"
import Search from "./routes/Search"

const rootElement = document.getElementById('root')

ReactDOM.render(
  <BrowserRouter>
    <Routes>
      <Route path='/' element={<App />} />
      <Route path='home' element={<Home />} />
      <Route path='search' element={<Search />} />
    </Routes>
  </BrowserRouter>,
  rootElement
);
