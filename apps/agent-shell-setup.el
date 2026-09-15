;;; agent-shell-setup.el --- Setup for agent-shell package. -*- lexical-binding: t; -*-

;;; Commentary:
;; agent-sheill is a native Emacs shell to interact with LLM agents powered by ACP (Agent Client Protocol).

;; With agent-shell, you can chat with the likes of Gemini CLI, Claude Code, Auggie, Mistral Vibe, or any other ACP-driven agent.

;;; Code:

(use-package acp
  :ensure t
  :defer t)

(use-package agent-shell
  :ensure t
  :defer t
  :after acp
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-anthropic-claude-environment
   '("CLAUDE_CODE_EXECUTABLE=/home/nane/.local/bin/claude"))
  (agent-shell-header-style 'text))


(use-package agent-review
  :ensure (:type git :host github :repo "nineluj/agent-review")
  :after agent-shell)

(use-package agent-shell-sidebar
  :ensure (:url "https://github.com/cmacrae/agent-shell-sidebar")
  :after agent-shell)

(use-package agent-shell-manager
  :ensure (:url "https://github.com/jethrokuan/agent-shell-manager")
  :after agent-shell)

(provide 'agent-shell-setup)
;;; agent-shell-setup.el ends here
