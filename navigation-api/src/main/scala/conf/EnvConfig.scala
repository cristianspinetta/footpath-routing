package conf

import com.typesafe.config.Config

trait EnvConfig {
  def config: Config
}
