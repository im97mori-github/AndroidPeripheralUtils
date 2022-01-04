package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.bluetooth.BluetoothGattCharacteristic;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.BloodPressureMeasurementSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureMeasurementSettingActivity extends AppCompatActivity {

    private BloodPressureMeasurementSettingViewModel mViewModel;

    private BloodPressureMeasurementSettingActivityBinding mBinding;

    private final ActivityResultLauncher<Pair<String, Integer>> mStartClientCharacteristicConfigurationSettingActivity = registerForActivityResult(new ClientCharacteristicConfigurationLauncherContract()
            , result -> mViewModel.setClientCharacteristicConfigurationDescriptorJson(result));

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this).get(BloodPressureMeasurementSettingViewModel.class);

        mBinding = BloodPressureMeasurementSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsMmhg(this, check -> mBinding.unitRadioGroup.check(check ? R.id.mmhgRadioButton : R.id.kpaRadioButton));
        mBinding.unitRadioGroup.setOnCheckedChangeListener((group, checkedId) -> mViewModel.updateIsMmhg(checkedId == R.id.mmhgRadioButton));
        mViewModel.observeUnit(this, charSequence -> {
            mBinding.systolic.setSuffixText(charSequence);
            mBinding.diastolic.setSuffixText(charSequence);
            mBinding.meanArterialPressure.setSuffixText(charSequence);
        });

        mViewModel.observeSystolic(this, charSequence -> setTextDistinct(mBinding.systolicEdit, charSequence));
        mViewModel.observeSystolicErrorString(this, charSequence -> mBinding.systolic.setError(charSequence));
        mBinding.systolicEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateSystolic(editable)));

        mViewModel.observeDiastolic(this, charSequence -> setTextDistinct(mBinding.diastolicEdit, charSequence));
        mViewModel.observeDiastolicErrorString(this, charSequence -> mBinding.diastolic.setError(charSequence));
        mBinding.diastolicEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateDiastolic(editable)));

        mViewModel.observeMeanArterialPressure(this, charSequence -> setTextDistinct(mBinding.meanArterialPressureEdit, charSequence));
        mViewModel.observeMeanArterialPressureErrorString(this, charSequence -> mBinding.meanArterialPressure.setError(charSequence));
        mBinding.meanArterialPressureEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateMeanArterialPressure(editable)));

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

        mViewModel.observeTimeStampYear(this, charSequence -> setTextDistinct(mBinding.timeStampYearEdit, charSequence));
        mViewModel.observeTimeStampYearErrorString(this, charSequence -> mBinding.timeStampYear.setError(charSequence));
        mBinding.timeStampYearEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateTimeStampYear(editable)));

        mBinding.timeStampMonthEdit.performValidation();
        mBinding.timeStampMonthEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeMonthList()));
        mViewModel.observeTimeStampMonth(this, charSequence -> setTextDistinct(mBinding.timeStampMonthEdit, charSequence));
        mBinding.timeStampMonthEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMonth(position));

        mBinding.timeStampDayEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeDayList()));
        mViewModel.observeTimeStampDay(this, charSequence -> setTextDistinct(mBinding.timeStampDayEdit, charSequence));
        mBinding.timeStampDayEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampDay(position));

        mBinding.timeStampHoursEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeHoursList()));
        mViewModel.observeTimeStampHours(this, charSequence -> setTextDistinct(mBinding.timeStampHoursEdit, charSequence));
        mBinding.timeStampHoursEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampHours(position));

        mBinding.timeStampMinutesEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeMinutesList()));
        mViewModel.observeTimeStampMinutes(this, charSequence -> setTextDistinct(mBinding.timeStampMinutesEdit, charSequence));
        mBinding.timeStampMinutesEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMinutes(position));

        mBinding.timeStampSecondsEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeSecondsList()));
        mViewModel.observeTimeStampSeconds(this, charSequence -> setTextDistinct(mBinding.timeStampSecondsEdit, charSequence));
        mBinding.timeStampSecondsEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampSeconds(position));

        mViewModel.observeIsPulseRateSupported(this, check -> {
            mBinding.isPulseRateSupported.setChecked(check);
            mBinding.pulseRate.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isPulseRateSupported.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateIsPulseRateSupported(isChecked));
        mViewModel.observePulseRate(this, charSequence -> setTextDistinct(mBinding.pulseRateEdit, charSequence));
        mViewModel.observePulseRateErrorString(this, charSequence -> mBinding.pulseRate.setError(charSequence));
        mBinding.pulseRateEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updatePulseRate(editable)));

        mViewModel.observeIsUserIdSupported(this, check -> {
            mBinding.isUserIdSupported.setChecked(check);
            mBinding.userId.setVisibility(check ? View.VISIBLE : View.GONE);
        });
        mBinding.isUserIdSupported.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateIsUserIdSupported(isChecked));
        mViewModel.observeUserId(this, charSequence -> setTextDistinct(mBinding.userIdEdit, charSequence));
        mViewModel.observeUserIdErrorString(this, charSequence -> mBinding.userId.setError(charSequence));
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
        mViewModel.observeBodyMovementDetection(this, charSequence -> setTextDistinct(mBinding.bodyMovementDetectionEdit, charSequence));
        mBinding.bodyMovementDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateBodyMovementDetection(position));

        mBinding.cuffFitDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideCuffFitDetectionList()));
        mViewModel.observeCuffFitDetection(this, charSequence -> setTextDistinct(mBinding.cuffFitDetectionEdit, charSequence));
        mBinding.cuffFitDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateCuffFitDetection(position));

        mBinding.irregularPulseDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideIrregularPulseDetectionList()));
        mViewModel.observeIrregularPulseDetection(this, charSequence -> setTextDistinct(mBinding.irregularPulseDetectionEdit, charSequence));
        mBinding.irregularPulseDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateIrregularPulseDetection(position));

        mBinding.pulseRateRangeDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.providePulseRateRangeDetectionList()));
        mViewModel.observePulseRateRangeDetection(this, charSequence -> setTextDistinct(mBinding.pulseRateRangeDetectionEdit, charSequence));
        mBinding.pulseRateRangeDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updatePulseRateRangeDetection(position));

        mBinding.measurementPositionDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideMeasurementPositionDetectionList()));
        mViewModel.observeMeasurementPositionDetection(this, charSequence -> setTextDistinct(mBinding.measurementPositionDetectionEdit, charSequence));
        mBinding.measurementPositionDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateMeasurementPositionDetection(position));

        mViewModel.observeHasClientCharacteristicConfigurationDataJson(this, check -> mBinding.clientCharacteristicConfigurationCardView.setChecked(check));

        mViewModel.observeClientCharacteristicConfiguration(this, s -> mBinding.clientCharacteristicConfiguration.setText(s));
        mBinding.clientCharacteristicConfigurationSettingButton.setOnClickListener(v ->
                mStartClientCharacteristicConfigurationSettingActivity.launch(Pair.create(mViewModel.getClientCharacteristicConfigurationDescriptorJson()
                        , BluetoothGattCharacteristic.PROPERTY_INDICATE)));

        mViewModel.observeIndicationCount(this, charSequence -> setTextDistinct(mBinding.indicationCountEdit, charSequence));
        mViewModel.observeIndicationCountErrorString(this, charSequence -> mBinding.indicationCount.setError(charSequence));
        mBinding.indicationCountEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateIndicationCount(editable)));

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {

            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.findItem(R.id.save).setEnabled(mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (menuItem.getItemId() == R.id.save) {
                    mViewModel.observeSave(intent -> {
                        setResult(RESULT_OK, intent);
                        finish();
                    }, throwable
                            -> Toast.makeText(BloodPressureMeasurementSettingActivity.this
                            , throwable.getMessage()
                            , Toast.LENGTH_SHORT).show());
                    result = true;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> {
                    mBinding.rootContainer.setVisibility(View.VISIBLE);
                    mBinding.topAppBar.invalidateMenu();
                }
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}
