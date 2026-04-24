# Ansible playbook to provision and deploy obx playground

This directory contains a small Ansible playbook and supporting files to provision a VPS and deploy the obx playground using Docker Compose + Nginx + Certbot.

Quick start

1. Install Ansible on your control machine (macOS/Linux):

   ```bash
   pip install --user ansible
   ```

2. Edit `inventory.ini` to point to your VPS (set `ansible_host` and `ansible_user`).

3. Run the playbook (from the repo root):

   ```bash
   ansible-playbook -i infra/ansible/inventory.ini infra/ansible/playbook.yml \
     -e "deploy_user=obx image=yourregistry/obx:latest domain=playground.example.com email=you@example.com"
   ```

Notes
- The playbook will:
  - install `docker.io`, `docker-compose-plugin`, `nginx`, `ufw` and `certbot` on the VPS
  - create a deploy user, deploy directory and copy `docker-compose.yml`
  - copy the nginx site and enable it
  - bring up the Docker Compose service using the provided image
  - poll the local health endpoint `/api/version` until it responds
  - optionally request TLS with Certbot if `domain` and `email` are provided

- The playbook uses the files in `infra/ansible/files/` (docker-compose.yml and nginx config). If you modify `deploy/docker-compose.yml` keep the copy under `infra/ansible/files/` in sync or update the playbook copy.

Security
- The playbook creates the deploy user and adds it to the `docker` group. Ensure your SSH keys are managed appropriately and that the deploy user has a secure key.

Troubleshooting
- If the healthcheck fails, check container logs on the VPS:
  ```bash
  ssh obx@your-vps
  docker compose -f /home/obx/deploy/docker-compose.yml logs --tail=200
  ```

