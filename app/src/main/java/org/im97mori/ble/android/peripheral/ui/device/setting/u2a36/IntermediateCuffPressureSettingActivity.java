package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import android.bluetooth.BluetoothGattCharacteristic;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.IntermediateCuffPressureSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.ListItemAdapter;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class IntermediateCuffPressureSettingActivity extends BaseActivity {

    private IntermediateCuffPressureSettingViewModel mViewModel;

    private IntermediateCuffPressureSettingActivityBinding mBinding;

    private final ActivityResultLauncher<Pair<String, Integer>> mStartClientCharacteristicConfigurationSettingActivity = registerForActivityResult(new ClientCharacteristicConfigurationLauncherContract()
            , result -> mViewModel.setClientCharacteristicConfigurationDescriptorJson(result));

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(IntermediateCuffPressureSettingViewModel.class);

        mBinding = IntermediateCuffPressureSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsMmhg(this, check -> mBinding.unitRadioGroup.check(check ? R.id.mmhgRadioButton : R.id.kpaRadioButton));
        mBinding.unitRadioGroup.setOnCheckedChangeListener((group, checkedId) -> mViewModel.updateIsMmhg(checkedId == R.id.mmhgRadioButton));
        mViewModel.observeUnit(this, charSequence -> mBinding.currentCuffPressure.setSuffixText(charSequence));

        mViewModel.observeCurrentCuffPressure(this, charSequence -> distinctSetText(mBinding.currentCuffPressureEdit, charSequence));
        mViewModel.observeCurrentCuffPressureError(this, charSequence -> mBinding.currentCuffPressure.setError(charSequence));
        mBinding.currentCuffPressureEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateCurrentCuffPressure(editable)));

        mViewModel.observeIsTimeStampSupported(this, check -> {
            mBinding.isTimeStampSupported.setChecked(check);

            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.timeStampYear.setVisibility(visibility);
            mBinding.timeStampMonth.setVisibility(visibility);
            mBinding.timeStampDay.setVisibility(visibility);
            mBinding.timeStampHours.setVisibility(visibility);
            mBinding.timeStampMinutes.setVisibility(visibility);
            mBinding.timeStampSeconds.setVisibility(visibility);
        });
        mBinding.isTimeStampSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsTimeStampSupported(isChecked));

        mViewModel.observeTimeStampYear(this, charSequence -> distinctSetText(mBinding.timeStampYearEdit, charSequence));
        mViewModel.observeTimeStampYearError(this, charSequence -> mBinding.timeStampYear.setError(charSequence));
        mBinding.timeStampYearEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateTimeStampYear(editable)));

        mBinding.timeStampMonthEdit.setAdapter(new ListItemAdapter(this, mViewModel.provideDateTimeMonthList()));
        mViewModel.observeTimeStampMonth(this, charSequence -> distinctSetText(mBinding.timeStampMonthEdit, charSequence));
        mBinding.timeStampMonthEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMonth(position));

        mBinding.timeStampDayEdit.setAdapter(new ListItemAdapter(this, mViewModel.provideDateTimeDayList()));
        mViewModel.observeTimeStampDay(this, charSequence -> distinctSetText(mBinding.timeStampDayEdit, charSequence));
        mBinding.timeStampDayEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampDay(position));

        mBinding.timeStampHoursEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeHoursList()));
        mViewModel.observeTimeStampHours(this, charSequence -> distinctSetText(mBinding.timeStampHoursEdit, charSequence));
        mBinding.timeStampHoursEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampHours(position));

        mBinding.timeStampMinutesEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeMinutesList()));
        mViewModel.observeTimeStampMinutes(this, charSequence -> distinctSetText(mBinding.timeStampMinutesEdit, charSequence));
        mBinding.timeStampMinutesEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMinutes(position));

        mBinding.timeStampSecondsEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeSecondsList()));
        mViewModel.observeTimeStampSeconds(this, charSequence -> distinctSetText(mBinding.timeStampSecondsEdit, charSequence));
        mBinding.timeStampSecondsEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampSeconds(position));

        mViewModel.observeIsPulseRateSupported(this, check -> {
            mBinding.isPulseRateSupported.setChecked(check);
            mBinding.pulseRate.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isPulseRateSupported.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateIsPulseRateSupported(isChecked));
        mViewModel.observePulseRate(this, charSequence -> distinctSetText(mBinding.pulseRateEdit, charSequence));
        mViewModel.observePulseRateError(this, charSequence -> mBinding.pulseRate.setError(charSequence));
        mBinding.pulseRateEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updatePulseRate(editable)));

        mViewModel.observeIsUserIdSupported(this, check -> {
            mBinding.isUserIdSupported.setChecked(check);
            mBinding.userId.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isUserIdSupported.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateIsUserIdSupported(isChecked));
        mViewModel.observeUserId(this, charSequence -> distinctSetText(mBinding.userIdEdit, charSequence));
        mViewModel.observeUserIdError(this, charSequence -> mBinding.userId.setError(charSequence));
        mBinding.userIdEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateUserId(editable)));

        mViewModel.observeIsMeasurementStatusSupported(this, check -> {
            mBinding.isMeasurementStatusSupported.setChecked(check);
            int visibility = check ? View.VISIBLE : View.GONE;
            mBinding.bodyMovementDetection.setVisibility(visibility);
            mBinding.cuffFitDetection.setVisibility(visibility);
            mBinding.irregularPulseDetection.setVisibility(visibility);
            mBinding.pulseRateRangeDetection.setVisibility(visibility);
            mBinding.measurementPositionDetection.setVisibility(visibility);
        });
        mBinding.isMeasurementStatusSupported.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsMeasurementStatusSupported(isChecked));

        mBinding.bodyMovementDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideBodyMovementDetectionList()));
        mViewModel.observeBodyMovementDetection(this, charSequence -> distinctSetText(mBinding.bodyMovementDetectionEdit, charSequence));
        mBinding.bodyMovementDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateBodyMovementDetection(position));

        mBinding.cuffFitDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideCuffFitDetectionList()));
        mViewModel.observeCuffFitDetection(this, charSequence -> distinctSetText(mBinding.cuffFitDetectionEdit, charSequence));
        mBinding.cuffFitDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateCuffFitDetection(position));

        mBinding.irregularPulseDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideIrregularPulseDetectionList()));
        mViewModel.observeIrregularPulseDetection(this, charSequence -> distinctSetText(mBinding.irregularPulseDetectionEdit, charSequence));
        mBinding.irregularPulseDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateIrregularPulseDetection(position));

        mBinding.pulseRateRangeDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.providePulseRateRangeDetectionList()));
        mViewModel.observePulseRateRangeDetection(this, charSequence -> distinctSetText(mBinding.pulseRateRangeDetectionEdit, charSequence));
        mBinding.pulseRateRangeDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updatePulseRateRangeDetection(position));

        mBinding.measurementPositionDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideMeasurementPositionDetectionList()));
        mViewModel.observeMeasurementPositionDetection(this, charSequence -> distinctSetText(mBinding.measurementPositionDetectionEdit, charSequence));
        mBinding.measurementPositionDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateMeasurementPositionDetection(position));

        mViewModel.observeHasClientCharacteristicConfigurationData(this, check -> mBinding.clientCharacteristicConfigurationCardView.setChecked(check));

        mViewModel.observeClientCharacteristicConfiguration(this, s -> mBinding.clientCharacteristicConfiguration.setText(s));
        mBinding.clientCharacteristicConfigurationSettingButton.setOnClickListener(v ->
                mStartClientCharacteristicConfigurationSettingActivity.launch(Pair.create(mViewModel.getClientCharacteristicConfigurationDescriptorJson()
                        , BluetoothGattCharacteristic.PROPERTY_NOTIFY)));

        mViewModel.observeNotificationCount(this, charSequence -> distinctSetText(mBinding.notificationCountEdit, charSequence));
        mViewModel.observeNotificationCountError(this, charSequence -> mBinding.notificationCount.setError(charSequence));
        mBinding.notificationCountEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateNotificationCount(editable)));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe(() -> mBinding.rootContainer.setVisibility(View.VISIBLE), throwable -> LogUtils.stackLog(throwable.getMessage())));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribe(intent -> {
                        setResult(RESULT_OK, intent);
                        finish();
                    }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show()));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
