package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;


@Slf4j
@RestController
@RequestMapping("amqp")
@Validated
public class AmqpController {


    @Resource
    private OpLocationMapper opLocationMapper;

    @Autowired
    private RabbitTemplate rabbitTemplate;

    @GetMapping(value = "/t1/{id}")
    public ResponseEntity<?> testSendTopic1(@PathVariable("id") Long id) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(id);
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setId(opLocationEntity.getId());
        opLocationDTO.setName(opLocationEntity.getName());


        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, "LOCATION.ADD", opLocationDTO);
        return ResponseEntity.ok(opLocationDTO);
    }

    @GetMapping(value = "/t2/{id}")
    public ResponseEntity<OpLocationDTO> testSendTopic2(@PathVariable("id") Long id) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(id);
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setId(opLocationEntity.getId());
        opLocationDTO.setName(opLocationEntity.getName());


        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, "LOCATION.DELETE", opLocationDTO);
        return ResponseEntity.ok(opLocationDTO);

    }

    @GetMapping(value = "/t3/{id}")
    public ResponseEntity<?> testSendTopic3(@PathVariable("id") Long id) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(id);
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setId(opLocationEntity.getId());
        opLocationDTO.setName(opLocationEntity.getName());
        String jsonString = JSON.toJSONString(opLocationDTO);
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, LOCATION_UPDATE, jsonString);
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE_FANOUT + RabbitBean.RABBITMQ_VERSION_SUFFIX, LOCATION_UPDATE, jsonString);
        return ResponseEntity.ok(opLocationEntity);
    }

    @GetMapping(value = "/t4/{id}")
    public ResponseEntity<?> testSendDirect4(@PathVariable("id") Long id) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(id);
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setId(opLocationEntity.getId());
        opLocationDTO.setName(opLocationEntity.getName());
        rabbitTemplate.convertAndSend(DIRECT_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, LOCATION_UPDATE, opLocationDTO);
        return ResponseEntity.ok(opLocationEntity);
    }


}