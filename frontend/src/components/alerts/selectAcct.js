import swal from '@sweetalert/with-react'
import './alert.css'
import reduceToken from '../../utils/reduceToken';

export default async (accts, sa1, sa2, sa3) => {
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
    console.log(e.target.value)
    await sa1(e.target.value);
    await sa2(e.target.value);
    await sa3(e.target.value);
    value = e.target.value
  }
  return swal({
    text: "Choose Public Key you intend to use",
    title: "Select Account",
    content: {
      element: selectList
    }

  })
// var value;
// const select = document.createElement('select');
//  select.className = 'select-custom'
//  const option1 = document.createElement('option');
//  const option2 = document.createElement('option');
//  const option3 = document.createElement('option');
//
// option1.innerHTML = 'Option 1';
// option1.value = "1"
// option2.innerHTML = 'Option 2';
// option2.value = "2"
// option3.innerHTML = 'Option 3 ';
// option3.value = "3"
//
// select.appendChild(option1);
// select.appendChild(option2);
// select.appendChild(option3);
//
// select.onchange = function selectChanged(e) {
//   value = e.target.value
// }
//
// swal({
//   content: {
//     element: select,
//   }
// }).then(function() {
//   console.log()
// })

};
