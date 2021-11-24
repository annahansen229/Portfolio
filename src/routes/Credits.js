import "bootstrap/dist/css/bootstrap.min.css";
import "../App.css";
import Navbar from "../components/navbar.js";

function Credits() {
  return (
    <div className="Credits">
      <Navbar />
      <body className="creditsbody">
      <h1 className="creditsheader"> By Jason Bockover and Anna Hansen</h1>
      </body>
    </div>
  );
}

export default Credits;
