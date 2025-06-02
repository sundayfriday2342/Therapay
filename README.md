# 🧠 Therapay Smart Contract

A decentralized escrow system for mental health therapy sessions on Stacks blockchain.

## 🎯 Overview

Therapay facilitates secure payments between patients and therapists by implementing an escrow system that:
- ✅ Holds payment in escrow until session completion
- 🔒 Ensures therapist verification
- 💰 Manages session payments automatically
- 📊 Tracks completed sessions

## 🚀 Features

- Therapist registration and verification
- Session booking with automatic payment escrow
- Session completion and payment release
- Rate management for therapists
- Session history tracking

## 💡 How It Works

1. Therapists register with their rate
2. Contract owner verifies therapists
3. Patients book sessions (payment goes to escrow)
4. Therapists complete sessions
5. Smart contract releases payment

## 📝 Usage

### For Therapists

```clarity
;; Register as therapist (rate in STX)
(contract-call? .therapay register-therapist u100)

;; Update session rate
(contract-call? .therapay update-rate u150)

;; Complete a session
(contract-call? .therapay complete-session u1)
```

### For Patients

```clarity
;; Book a session
(contract-call? .therapay book-session 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### For Contract Owner

```clarity
;; Verify a therapist
(contract-call? .therapay verify-therapist 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔍 Query Functions

- `get-session`: Get session details
- `get-therapist-info`: Get therapist information
- `get-session-count`: Get total sessions for a patient

## ⚠️ Requirements

- Clarinet
- Stacks Wallet
- STX tokens for transactions
```


