import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import { Lucid, TxHash, Lovelace, Constr, SpendingValidator, Data } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'


const Helios: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")


  useEffect(() => {
    if (lucid) {
      ;
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])

  const lockUtxo = async () => {
    if (lucid) {
     
    }

  }

  const redeemUtxo = async () => {
    if (lucid) {
     
    }
  }

  return (
    <div className="px-10">
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/" className="btn btn-ghost normal-case text-xl">Cardano</Link>
        </div>
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div>Address: {walletStore.address}</div>
      <div className='m-10'>
        <p> Emurgo example
        </p>

      </div>
      <div className="mx-40 my-10">
        <button className="btn btn-primary m-5" onClick={() => { lockUtxo() }} >Deposit</button>
        <button className="btn btn-secondary m-5" onClick={() => { redeemUtxo() }}>Unlock</button>
      </div>
    </div>
  )
}

export default Helios