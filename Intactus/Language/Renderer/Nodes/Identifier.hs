
module Language.Renderer.Nodes.Identifier ( renderASTIdentifer, identifierPrettyName ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import Infrastructure.Text.Layout
import Language.FrontEnd.Types

-- ─── IDENTIFIER PRETTY NAME ─────────────────────────────────────────────────────

identifierPrettyName :: String -> String
identifierPrettyName name =
    case name of
        "Alpha"     -> "α"
        "Beta"      -> "β"
        "Gamma"     -> "γ"
        "Delta"     -> "δ"
        "Epsilon"   -> "ε"
        "Zeta"      -> "ζ"
        "Eta"       -> "η"
        "Theta"     -> "θ"
        "Iota"      -> "ι"
        "Kappa"     -> "κ"
        "Lambda"    -> "λ"
        "Mu"        -> "μ"
        "Nu"        -> "ν"
        "Xi"        -> "ξ"
        "Omicron"   -> "ο"
        "Pi"        -> "π"
        "Rho"       -> "ρ"
        "Sigma"     -> "σ"
        "Tau"       -> "τ"
        "Upsilon"   -> "υ"
        "Phi"       -> "φ"
        "Chi"       -> "χ"
        "Psi"       -> "ψ"
        "Omega"     -> "ω"

        "Alpha'"    -> "Α"
        "Beta'"     -> "Β"
        "Gamma'"    -> "Γ"
        "Delta'"    -> "Δ"
        "Epsilon'"  -> "Ε"
        "Zeta'"     -> "Ζ"
        "Eta'"      -> "Η"
        "Theta'"    -> "Θ"
        "Iota'"     -> "Ι"
        "Kappa'"    -> "Κ"
        "Lambda'"   -> "Λ"
        "Mu'"       -> "Μ"
        "Nu'"       -> "Ν"
        "Xi'"       -> "Ξ"
        "Omicron'"  -> "Ο"
        "Pi'"       -> "Π"
        "Rho'"      -> "Ρ"
        "Sigma'"    -> "Σ"
        "Tau'"      -> "Τ"
        "Upsilon'"  -> "Υ"
        "Phi'"      -> "Φ"
        "Chi'"      -> "Χ"
        "Psi'"      -> "Ψ"
        "Omega'"    -> "Ω"

        _           -> name

-- ─── RENDER ─────────────────────────────────────────────────────────────────────

renderASTIdentifer :: String -> SpacedBox
renderASTIdentifer x =
    spacedBox $ identifierPrettyName x

-- ────────────────────────────────────────────────────────────────────────────────
