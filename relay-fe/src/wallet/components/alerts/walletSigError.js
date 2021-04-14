import swal from '@sweetalert/with-react'
import './alert.css'

export default async () => {
  return await swal({
    text: "You cancelled the transaction or did not sign it correctly. Please make sure you sign with the keys of the account linked in Kadenaswap",
    title: "Wallet Signing Failure"
  })
};
