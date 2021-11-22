import { createContext } from "react";

export const FormContext = createContext({
    characterName: '',
    species: '',
    ancestry: '',
    gender: '',
    house: '',
    living: '',
    book: '',
})

export const CharactersContext = createContext([])

export const BooksContext = createContext([])

