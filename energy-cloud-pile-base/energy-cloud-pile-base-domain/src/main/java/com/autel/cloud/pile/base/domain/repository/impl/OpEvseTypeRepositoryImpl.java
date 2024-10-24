package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpEvseTypeRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseTypeMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class OpEvseTypeRepositoryImpl extends ServiceImpl<OpEvseTypeMapper, OpEvseTypeEntity> implements OpEvseTypeRepository {

}
