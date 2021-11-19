import { Link } from "react-router-dom"
import "bootstrap/dist/css/bootstrap.min.css";
import Image from "../images/harry-potter-logo-tutorial-final.jpg";


export default function Navbar() {
    return <nav>   
        <img class="logo" src={Image} alt="Harry Potter Logo" />
        <Link to='/'>Home</Link> |{" "}
        <Link to='/search'>Characters</Link> |{" "}
        <Link to='/spells'>Spells</Link> |{" "}
        <Link to='/potions'>potions</Link> |{" "}
    </nav>
}