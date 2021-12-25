package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeMainViewModel extends MainViewModel {

    PublishProcessor<List<Device>> getDeviceListProcessor = PublishProcessor.create();

    Consumer<Boolean> deleteAllDevicesConsumer;

    @Inject
    FakeMainViewModel(@NonNull DeviceRepository deviceRepository) {
        super(deviceRepository);
    }

    @NonNull
    public Flowable<List<Device>> getDeviceList() {
        return getDeviceListProcessor;
    }


    @NonNull
    @Override
    public Completable deleteAllDevices() {
        if (deleteAllDevicesConsumer != null) {
            deleteAllDevicesConsumer.accept(true);
        }
        return super.deleteAllDevices();
    }

    public void setDeleteAllDevicesConsumer(@NonNull Consumer<Boolean> consumer) {
        deleteAllDevicesConsumer = consumer;
    }

}