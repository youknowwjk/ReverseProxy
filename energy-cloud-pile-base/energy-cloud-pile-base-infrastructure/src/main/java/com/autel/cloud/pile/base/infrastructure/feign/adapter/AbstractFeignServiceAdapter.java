package com.autel.cloud.pile.base.infrastructure.feign.adapter;


import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.webhook.WeChatClient;
import com.autel.cloud.base.webhook.dto.Text;
import com.autel.cloud.base.webhook.dto.TextDecorator;
import com.autel.cloud.base.webhook.message.TextMessage;
import com.autel.cloud.base.webhook.message.WeChatMessage;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.bill.dto.ResultDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.Environment;

import javax.annotation.Resource;
import java.util.Objects;

@Slf4j
public abstract class AbstractFeignServiceAdapter {


    @Resource
    private Environment environment;

    @Resource
    protected WeChatClient weChatClient;

    protected WeChatMessage buildTextMessage(String content) {
        Text text = new TextDecorator();
        TextMessage weChatMessage = new TextMessage(text);
        text.setContent("【" + environment.getProperty("NACOS_NS", "N/A") + "】" + content);
        return weChatMessage;
    }


    public String getEnvironment() {
        return environment.getProperty("NACOS_NS", "N/A");
    }

    protected <T> T handle(Result<T> result) {
        if (Objects.isNull(result)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        if (Objects.isNull(result.getData())) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        return result.getData();
    }


    protected <T> T nullableHandle(Result<T> result) {
        if (Objects.isNull(result)) {
            return null;
        }
        if (Objects.isNull(result.getData())) {
            return null;
        }
        return result.getData();
    }

    protected <T> T nullableHandle(ResultDTO<T> result) {
        if (Objects.isNull(result)) {
            return null;
        }
        if (Objects.isNull(result.getData())) {
            return null;
        }
        return result.getData();
    }
}
