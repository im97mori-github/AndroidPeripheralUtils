package org.im97mori.ble.android.peripheral.ui.device.setting.u2a29;

import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import java.util.LinkedList;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class ManufacturerNameStringSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_MANUFACTURER_NAME_STRING = "KEY_MANUFACTURER_NAME_STRING";
    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<String> mManufacturerNameString;
    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    private final MutableLiveData<Intent> mSavedData;

    @Inject
    public ManufacturerNameStringSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);
        mManufacturerNameString = savedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING);
        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                mCharacteristicData = Utils.byteToParcelable(intent.getByteArrayExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString()), CharacteristicData.CREATOR);

                if (mCharacteristicData == null) {
                    mCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                            , BluetoothGattCharacteristic.PROPERTY_READ
                            , BluetoothGattCharacteristic.PERMISSION_READ
                            , new LinkedList<>()
                            , BluetoothGatt.GATT_SUCCESS
                            , 0
                            , null
                            , -1);
                }
            }

            if (mIsErrorResponse.getValue() == null) {
                mIsErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
            }

            if (mManufacturerNameString.getValue() == null) {
                if (mCharacteristicData.data == null) {
                    mManufacturerNameString.postValue(null);
                } else {
                    mManufacturerNameString.postValue(new ManufacturerNameString(mCharacteristicData.data).getManufacturerName());
                }
            }

            if (mResponseCode.getValue() == null) {
                mResponseCode.postValue(String.valueOf(mCharacteristicData.responseCode));
            }

            if (mResponseDelay.getValue() == null) {
                mResponseDelay.postValue(String.valueOf(mCharacteristicData.delay));
            }

            emitter.onComplete();
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsErrorResponse).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameString).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameStringErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameString).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getManufacturerNameStringErrorString(s)));
    }

    @MainThread
    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCodeErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseCodeErrorString(s)));
    }

    @MainThread
    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner, observer);
    }

    @MainThread
    public void observeResponseDelayErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseDelayErrorString(s)));
    }

    @MainThread
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsErrorResponse(boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateManufacturerNameString(@NonNull String text) {
        mManufacturerNameString.setValue(text);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                String manufacturerNameString = mManufacturerNameString.getValue();

                if (responseDelay != null && mDeviceSettingRepository.getResponseDelayErrorString(responseDelay) == null) {
                    mCharacteristicData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceSettingRepository.getResponseCodeErrorString(responseCode) == null) {
                            mCharacteristicData.data = null;
                            mCharacteristicData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(mCharacteristicData));

                            mSavedData.postValue(intent);
                            mCharacteristicData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        if (manufacturerNameString != null && mDeviceSettingRepository.getManufacturerNameStringErrorString(manufacturerNameString) == null) {
                            mCharacteristicData.data = new ManufacturerNameString(manufacturerNameString).getBytes();
                            mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;

                            Intent intent = new Intent();
                            intent.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(mCharacteristicData));

                            mSavedData.postValue(intent);
                            mCharacteristicData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    }
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }
}
