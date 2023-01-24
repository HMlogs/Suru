import type { NextPage } from "next";
import React, { useState, useEffect } from "react";
import styles from "../styles/TextRotator.module.css";

interface Props {
  words: React.ReactNode[];
}

const TextRotator: React.FC<Props> = ({ words }) => {
  return (
    <div className={styles.scroller}>
      <span className={styles.word}>
        {words.map((word, i) => (
          <div key={i}>{word}</div>
        ))}
      </span>
    </div>
  );
};

export default TextRotator;