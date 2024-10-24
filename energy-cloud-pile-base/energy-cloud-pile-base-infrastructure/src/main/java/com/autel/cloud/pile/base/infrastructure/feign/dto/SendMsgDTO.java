package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 吴超
 * @date 2021/6/25 18:49
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SendMsgDTO {
    private String msg;
    private String receiver;
}
