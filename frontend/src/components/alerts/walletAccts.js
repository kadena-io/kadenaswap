import swal from '@sweetalert/with-react'
import './loading.css'

export default async () => {
  return await swal({
    text: "Follow instructions in the wallet to share your accounts",
    title: "Please Approve",
    showSpinner: true,
    showCancelButton: false,
    showConfirmButton: false
  })
};
