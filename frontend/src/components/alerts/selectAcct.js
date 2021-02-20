import swal from '@sweetalert/with-react'
import './alert.css'
import reduceToken from '../../utils/reduceToken';

export default async (accts, sa1, sa2, sa3, isAcct) => {
  var value;
  //Create and append select list
  var selectList = document.createElement("select");
  selectList.id = "mySelect";
  //Create and append the options
  for (var i = 0; i < accts.length; i++) {
      var option = document.createElement("option");
      option.value = accts[i];
      option.text = reduceToken(accts[i]);
      selectList.appendChild(option);
  }
  selectList.onchange = async function selectChanged(e) {
    await sa1(e.target.value);
    await sa2(e.target.value);
    await sa3(e.target.value);
    value = e.target.value
  }
  return swal({
    text: "Choose Public Key you intend to use",
    title: `Select Account`,
    buttons: {
      confirm: true,
    },
    content: {
      element: selectList
    }

  })

};
